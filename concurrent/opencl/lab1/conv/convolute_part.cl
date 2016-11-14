
void kernel convolute_part(global const float* a_mat, 
                           global const float* b_mat, 
                           global float* c_mat, 
                           global const size_t* sizes) 
{
    size_t work_item_id, work_items_num, ratio, from, to;
    size_t a_size, b_size, total_size;
    size_t row, col;
    size_t i;
    int k, l;
    int h; // window size
    float b_val;
    float a_val;

    work_item_id = get_global_id(0);
    work_items_num = get_global_size(0);
    a_size = sizes[0];
    b_size = sizes[1];
    h = (b_size - 1) / 2;
    total_size = a_size * a_size;
    ratio = (total_size / work_items_num);
    from = ratio * work_item_id;
    to  = ratio * (work_item_id + 1);
    if (to > total_size) {
        to = total_size;
    }

    // printf("a_size     = %d\n", a_size);
    // printf("b_size     = %d\n", b_size);
    // printf("h          = %d\n", h);
    // printf("total_size = %d\n", total_size);
    // printf("from       = %d\n", from);
    // printf("to         = %d\n", to);
    // printf("\n", to);


    // // printf("IN OPENCL!!!!");

    for (i = from; i < to; ++i) {
        row = i / a_size;
        col = i % a_size;
        c_mat[i] = 0;
        for (k = -h; k <= h; ++k) {
                for (l = -h; l <= h; ++l) {
                        if (row + k < 0 || row + k >= a_size || col + l < 0 || col + l >= a_size) {
                                continue;
                        }
                        b_val = b_mat[(k + h) * b_size + l + h];
                        a_val = a_mat[(row + k) * a_size + col + l];
                        c_mat[i] += b_val * a_val;
                }
        }
    }
}
