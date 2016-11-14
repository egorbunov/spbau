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

std::string read_convolution_program() {
    std::ifstream file("convolute_one_cell.cl");
    std::string prog((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    return prog;
}

template<typename T>
struct matrix
{
private:
	const size_t m_size;
	T* m_buffer;

public:
	matrix(size_t sz): m_size(sz) {
		m_buffer = new T[m_size * m_size];
		memset(m_buffer, 0, sizeof(T) * m_size * m_size);
	}

	~matrix() {
		delete[] m_buffer;
	}

	T* buffer() {
		return m_buffer;
	}

	matrix(const matrix&) = delete;
	matrix& operator=(const matrix&) = delete;

	matrix(matrix&& other): m_size(other.m_size) {
		m_buffer = other.m_buffer;
		other.m_buffer = nullptr;
	}

	matrix& operator=(matrix&& other) = delete;


	size_t size() const {
		return m_size;
	}

	T& operator()(size_t i, size_t j) const {
		if (i > m_size || j > m_size) {
			throw std::runtime_error("index out of bounds");
		}
		return m_buffer[m_size * i + j];
	}

	friend std::ostream& operator<<(std::ostream& out, const matrix& m) {
		for (size_t i = 0; i < m.size(); ++i) {
			for (size_t j = 0; j < m.size(); ++j) {
				out << m(i, j) << " ";
			}
			out << std::endl;
		}
		return out;
	}
};

struct matrices
{
	matrix<float> a_mat;
	matrix<float> b_mat;
	matrix<float> c_mat;

	matrices(matrix<float>&& a_mat, matrix<float>&& b_mat, matrix<float>&& c_mat)
	: a_mat(std::move(a_mat)), b_mat(std::move(b_mat)), c_mat(std::move(c_mat)) 
	{}
};

matrices read_matrices() {
	const std::string f_name = "input.txt";
	std::ifstream in(f_name, std::ifstream::in);
	if (!in) {
		throw std::runtime_error("No input.txt file!");
	}
	size_t n = 0;
	size_t m = 0;
	in >> n >> m;
	matrix<float> a_mat(n);
	for (size_t i = 0; i < n; ++i) {
		for (size_t j = 0; j < n; ++j) {
			in >> a_mat(i, j);
		}
	}
	matrix<float> b_mat(m);
	for (size_t i = 0; i < m; ++i) {
		for (size_t j = 0; j < m; ++j) {
			in >> b_mat(i, j);
		}
	}
	matrix<float> c_mat(n);
	return matrices(std::move(a_mat), std::move(b_mat), std::move(c_mat));
}

template<typename T>
void write_answer(const matrix<T>& m) {
	const std::string f_name = "output.txt";
	std::ofstream out(f_name, std::ofstream::out);
	if (!out) {
		throw std::runtime_error("Can't open output.txt to write");
	}
	out << m;
}

int main() {
    auto cl_device = get_host_device();
    cl::Context context({cl_device});

    auto prog_str = read_convolution_program();
    cl::Program::Sources sources;
    sources.push_back({prog_str.c_str(), prog_str.length()});

    cl::Program cl_program(context, sources);
    if (cl_program.build({cl_device}) != CL_SUCCESS) {
        std::cout << "Error building: " << cl_program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(cl_device) << std::endl;
        exit(1);
    }

    auto matrices = read_matrices();

    size_t sizes[2] = {matrices.a_mat.size(), matrices.b_mat.size()};

    size_t flat_a_size = matrices.a_mat.size() * matrices.a_mat.size();
    size_t flat_b_size = matrices.b_mat.size() * matrices.b_mat.size();
    size_t flat_c_size = matrices.c_mat.size() * matrices.c_mat.size();

    // create buffers on device (allocate space on GPU)
    cl::Buffer buffer_a(context, CL_MEM_READ_WRITE, sizeof(float) * flat_a_size);
    cl::Buffer buffer_b(context, CL_MEM_READ_WRITE, sizeof(float) * flat_b_size);
    cl::Buffer buffer_c(context, CL_MEM_READ_WRITE, sizeof(float) * flat_c_size);
    cl::Buffer buffer_sizes(context, CL_MEM_READ_ONLY,  sizeof(size_t) * 2);


    // create a queue (a queue of commands that the GPU will execute)
    cl::CommandQueue queue(context, cl_device);
    // push write commands to queue
    queue.enqueueWriteBuffer(buffer_a, CL_TRUE, 0, sizeof(float) * flat_a_size, matrices.a_mat.buffer());
    queue.enqueueWriteBuffer(buffer_b, CL_TRUE, 0, sizeof(float) * flat_b_size, matrices.b_mat.buffer());
    queue.enqueueWriteBuffer(buffer_sizes, CL_TRUE, 0, sizeof(size_t) * 2, sizes);


    // RUN ZE KERNEL
    cl::Kernel convolute_part(cl_program, "convolute_part");
	convolute_part.setArg(0, buffer_a);
	convolute_part.setArg(1, buffer_b);
	convolute_part.setArg(2, buffer_c);
	convolute_part.setArg(3, buffer_sizes);
	queue.enqueueNDRangeKernel(convolute_part, 
		                       cl::NullRange, 
		                       cl::NDRange(matrices.c_mat.size()), // size of work items (one row --> one work item) 
		                       cl::NDRange(matrices.c_mat.size()); // one work group

    queue.enqueueReadBuffer(buffer_c, CL_TRUE, 0, sizeof(float) * flat_c_size, matrices.c_mat.buffer());


    write_answer(matrices.c_mat);

    return 0;
}
