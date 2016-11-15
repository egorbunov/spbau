
void kernel convolute_part(global const float* a_mat, 
                           global const float* b_mat, 
                           global float* c_mat, 
                           global const size_t* sizes) 
{
    size_t a_size = sizes[0];
    size_t b_size = sizes[1];
    int h = (b_size - 1) / 2;
    size_t row = get_global_id(0);
    size_t col = get_global_id(1);
    size_t flat_index = row * a_size + col;

    if (row >= a_size || col >= a_size) {
        return;
    }

    float sum = 0;
    for (int k = -h; k <= h; ++k) {
        for (int l = -h; l <= h; ++l) {
            if (row + k < 0 || row + k >= a_size || col + l < 0 || col + l >= a_size) {
                continue;
            }
            float bv = b_mat[(k + h) * b_size + l + h];
            float av = a_mat[(row + k) * a_size + col + l];
            sum += bv * av;
        }
    }

    c_mat[flat_index] = sum;
}
