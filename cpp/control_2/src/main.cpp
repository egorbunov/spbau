
#include <iostream>
#include <sstream>
#include "imp_double.h"

int main() {
	using namespace math;
 
    // constructors
    imp_double no_parameters{};
    imp_double one_parameter{ 1. };
    imp_double one_parameter_implicit = 2.;
    imp_double two_parameters(1.25e10, 1e-10);

    // @=
    no_parameters += 1.;
    no_parameters -= 1.;
    no_parameters *= 1.;
    no_parameters /= 1.;

    // imp_d @ d
    (void)(no_parameters + 1.);
    (void)(no_parameters - 1.);
    (void)(no_parameters * 1.);
    (void)(no_parameters / 1.);

    // imp_d @ imp_d
    (void)(no_parameters + one_parameter);
    (void)(no_parameters - one_parameter);
    (void)(no_parameters * one_parameter);
    (void)(no_parameters / one_parameter);

    // d @ imp_d
    (void)(1. + one_parameter);
    (void)(1. - one_parameter);
    (void)(1. * one_parameter);
    (void)(1. / one_parameter);

    // unary minus
    (void)(-one_parameter_implicit);

    // comparisons
    (void)(two_parameters <  2.);
    (void)(two_parameters <= 2.);
    (void)(two_parameters == 2.);
    (void)(two_parameters != 2.);
    (void)(two_parameters >  2.);
    (void)(two_parameters >= 2.);

    (void)(2. <  two_parameters);
    (void)(2. <= two_parameters);
    (void)(2. == two_parameters);
    (void)(2. != two_parameters);
    (void)(2. >  two_parameters);
    (void)(2. >= two_parameters);

    (void)one_parameter.get_delta();
    (void)one_parameter.get_value();

    std::stringstream stream;
    stream << two_parameters;
    stream >> one_parameter;

    imp_double x(1);
    imp_double y(2);
    x.add(y);

    std::cout << x << std::endl;

    std::cin.get();
    return 0;
}