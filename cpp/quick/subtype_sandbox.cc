
struct Arachnid {
	virtual void CatchPrey() final
	{
		std::cout << "\n [*] Arachnid catching Prey ...\n";
		
		build_trap();
		collect_prey();	 
	}
protected:
	virtual void build_trap() = 0;
	virtual void collect_prey() = 0;
};

struct TrapdoorSpider : Arachnid {
	void build_trap() override {
		std::cout << "\n [*] building trap ...";
	}
	void collect_prey() override {
		std::cout << "\n [*] collecting prey ...";
	}
};

__main__

TrapdoorSpider trap_spider;
trap_spider.CatchPrey();


