
__main__

auto shared_1 = std::make_shared<float>();
*shared_1 = 3.141592;
std::shared_ptr<float> shared_2 = shared_1;

std::cout << "\n *shared_2 = " << *shared_2;
std::cout << "\n use count = " << shared_2.use_count();



