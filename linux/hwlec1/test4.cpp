#include <algorithm>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <fstream>
#include <stdlib.h>

#include "distribution/e_distribution.h"
#include "distribution/emv_distribution.h"
#include "distribution/gaussian_copula.h"
#include "distribution/e_copula.h"
#include "hope.h"

#include "csv.h"

// #include <cstdion>
namespace cst {

result<void *> hope_t::read_model_vals(const std::string &model_vals_filename,
                                       std::vector<vec_t> &model_vals) {
  result<void *> load_models = load_csv(model_vals_filename, model_vals, 6);
  if (!load_models) {
    return result<void *>::error(load_models.err());
  }
  return result<void *>::ok(NULL);
}

