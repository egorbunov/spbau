#pragma once

template<class T>
class enumerator
{
public:
	virtual T get_current() const = 0;
	virtual bool move_next() = 0;
	virtual void reset() = 0;
	~enumerator() {};	

	virtual enumerator<T>* clone() const = 0;
};