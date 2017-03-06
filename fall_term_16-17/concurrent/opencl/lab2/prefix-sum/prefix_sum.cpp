#include <iostream>

#ifdef __APPLE__
#include <OpenCL/cl.hpp>
#else
#include <CL/cl.hpp>
#endif 

#include <vector>
#include <fstream>
#include <string>
#include <memory>
#include <stdexcept>
#include <ostream>

cl::Device get_host_device() {
	std::vector<cl::Platform> all_platforms;
    cl::Platform::get(&all_platforms);
    for (auto p : all_platforms) {
    	std::cout << "Platform found: " << p.getInfo<CL_PLATFORM_NAME>() << std::endl;
    }
    if (all_platforms.size()==0) {
        std::cout<<" No platforms found. Check OpenCL installation!\n";
        exit(1);
    }
    cl::Platform default_platform=all_platforms[0];
    std::cout << "Using platform: "<< default_platform.getInfo<CL_PLATFORM_NAME>() << std::endl;

    std::vector<cl::Device> all_devices;
    default_platform.getDevices(CL_DEVICE_TYPE_ALL, &all_devices);
    for (auto d : all_devices) {
    	std::cout << "Device found: " << d.getInfo<CL_DEVICE_NAME>() << std::endl;
    }
    if(all_devices.size()==0){
        std::cout<<" No devices found. Check OpenCL installation!\n";
        exit(1);
    }
    cl::Device default_device;
    if (all_devices.size() > 1) {
    	default_device = all_devices[1];
    } else {
    	default_device = all_devices[0];
    }
    std::cout<< "Using device: " << default_device.getInfo<CL_DEVICE_NAME>() << std::endl;
    return default_device;
}

std::string read_program() {
    std::ifstream file("prsum.cl");
    std::string prog((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    return prog;
}

std::vector<float> read_input() {
	const std::string f_name = "input.txt";
	std::ifstream in(f_name, std::ifstream::in);
	if (!in) {
		throw std::runtime_error("Can't open input.txt to read");
	}
	int size;
	in >> size;
	std::vector<float> vec(size);
	for (int i = 0; i < size; ++i) {
		in >> vec[i];
	}
	return vec;
}

void write_answer(const std::vector<float>& vec) {
	const std::string f_name = "output.txt";
	std::ofstream out(f_name, std::ofstream::out);
	if (!out) {
		throw std::runtime_error("Can't open output.txt to write");
	}
	for (auto v : vec) {
		out << v << " ";
	}
	out << std::endl;
}
using prsum_kernel = 
    cl::make_kernel<cl::Buffer&, cl::Buffer&, cl::Buffer&, cl::LocalSpaceArg, cl::LocalSpaceArg>;
using add_blocks_sums_kernel = 
    cl::make_kernel<cl::Buffer&, cl::Buffer&>;

std::vector<float> calc_prefix_sum(
    prsum_kernel prsum, 
    add_blocks_sums_kernel add_blocks_sums,
    cl::Context &context, 
    cl::CommandQueue &queue,
    std::vector<float> &input) {

    const size_t block_size = 256; // that is work group size
    const size_t lmem_sz = sizeof(float) * block_size;

    size_t block_cnt = input.size() / block_size + ((input.size() % block_size == 0) ? 0 : 1);
    size_t n = block_size * block_cnt;

    // input array buffer
    cl::Buffer input_buf(context, CL_MEM_READ_ONLY, sizeof(float) * n);
    // buffer to write sum of elements in each block (for recursive call)
    cl::Buffer block_sums_buf(context, CL_MEM_READ_WRITE, sizeof(float) * block_cnt);
    // buffer for resulting prefix sums for blocks sums array (recursive call result)
    cl::Buffer pr_sums_blocks_buf(context, CL_MEM_READ_ONLY, sizeof(float) * block_cnt);
    // buffer with resulting pref. sums
    cl::Buffer result_buf(context, CL_MEM_READ_WRITE, sizeof(float) * n);

    queue.enqueueWriteBuffer(input_buf, CL_TRUE, 0, sizeof(float) * n, &input[0]);

    cl::EnqueueArgs args(queue, cl::NDRange(n), cl::NDRange(block_size));
    // calculate prefix sums for blocks
    prsum(args, input_buf, block_sums_buf, result_buf, cl::Local(lmem_sz), cl::Local(lmem_sz)).wait();

    std::vector<float> result(n);
    if (block_cnt == 1) {
        // nothing to divide for calculation
        queue.enqueueReadBuffer(result_buf, CL_TRUE, 0, sizeof(float) * n, &result[0]);
        result.resize(input.size());
        return result;
    }
    std::vector<float> blocks_sums(block_cnt);
    queue.enqueueReadBuffer(block_sums_buf, CL_TRUE, 0, sizeof(float) * block_cnt, &blocks_sums[0]);

    // recursively calculating prefix sums for array of block sums
    std::vector<float> pr_sums_blocks = calc_prefix_sum(prsum, add_blocks_sums, context, queue, blocks_sums);
    queue.enqueueWriteBuffer(pr_sums_blocks_buf, CL_TRUE, 0, sizeof(float) * block_cnt, &pr_sums_blocks[0]);
    
    // updating calculated prefix sums by adding just calculated pr. sums of block-sum arrays
    add_blocks_sums(args, pr_sums_blocks_buf, result_buf).wait();
    queue.enqueueReadBuffer(result_buf, CL_TRUE, 0, sizeof(float) * n, &result[0]);
    
    result.resize(input.size());
    return result;
}

int main() {
    auto cl_device = get_host_device();
    cl::Context context({cl_device});

    auto prog_str = read_program();
    cl::Program::Sources sources;
    sources.push_back({prog_str.c_str(), prog_str.length()});

    cl::Program cl_program(context, sources);
    auto ret = cl_program.build({cl_device});
    if (ret != CL_SUCCESS) {
        std::cout << "Error building: " << cl_program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(cl_device) << std::endl;
        exit(1);
    }

    cl::CommandQueue queue(context, cl_device);
    cl::Kernel prsum_ker(cl_program, "prefix_sum");
    prsum_kernel prsum(prsum_ker);
    cl::Kernel add_blocks_sum_ker(cl_program, "add_blocks_sums");
    add_blocks_sums_kernel add_blocks_sum(add_blocks_sum_ker);

    std::vector<float> arr = read_input();
    std::vector<float> result = calc_prefix_sum(prsum, add_blocks_sum, context, queue, arr);
    write_answer(result);

    return 0;
}
