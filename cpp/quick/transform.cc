
__main__

std::vector<double> data = {23.23, 34.234, 44.23, 34.66, 55.33};
std::vector<double> log_data;
log_data.resize( data.size() );

std::transform( data.begin(), data.end(), log_data.begin(),
                [](const double& x) -> double { return log(x); } );

std::cout << std::endl;
util::print_vector( data, "data" );
std::cout << std::endl;
util::print_vector( log_data, "log_data" );
