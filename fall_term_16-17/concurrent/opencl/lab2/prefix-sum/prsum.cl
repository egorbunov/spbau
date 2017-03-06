void kernel prefix_sum(global float const *input, global float *block_sum,
                 global float *output,
                 local float *block_sums, local float *aux) {

    int glob_idx = get_global_id(0);
    int loc_idx = get_local_id(0);
    int block_size = get_local_size(0);
    int block_index = get_group_id(0);

    block_sums[loc_idx] = input[glob_idx];
    // using Hills-Steele scan to calc. sums in a block
    for(uint shift = 1; shift < block_size; shift <<= 1) {
        if (loc_idx >= shift) {
            aux[loc_idx] = block_sums[loc_idx] + block_sums[loc_idx - shift];
        } else {
            aux[loc_idx] = block_sums[loc_idx];
        }

        barrier(CLK_LOCAL_MEM_FENCE);
        
        local float *tmp = block_sums;
        block_sums = aux;
        aux = tmp;
    }

    output[glob_idx] = block_sums[loc_idx];
    if (loc_idx == block_size - 1) {
        block_sum[block_index] = output[glob_idx];
    }
}

void kernel add_blocks_sums(global float const *blocks_sums, global float *output) {
    int global_index = get_global_id(0);
    int block_index = get_group_id(0);
    if (block_index > 0) {
        output[global_index] += blocks_sums[block_index - 1];
    }
}
