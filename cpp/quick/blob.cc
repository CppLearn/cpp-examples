class Blob {
public:
	virtual ~Blob() = default;
	Blob() = default;
	Blob(const Blob&) = delete;
	Blob& operator=(const Blob&) = delete;

	virtual void step() = 0;
	virtual int total_steps() const = 0;
};

__main__




