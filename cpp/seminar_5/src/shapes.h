#pragma once

struct point;
struct circle;
struct rectangle;

struct shape {
	shape(double x, double y)
		: x_(x)
		, y_(y) 
	{}

	virtual void intersect(shape &other) = 0;
	virtual void intersect_impl(point &other) = 0;
	virtual void intersect_impl(rectangle &other) = 0;
	virtual void intersect_impl(circle &other) = 0;
	virtual ~shape() {}
protected:
	double x_;
	double y_; 
};

struct point: shape {
	point(double x, double y)
		: shape(x, y)
	{}
	~point() {}

	void intersect(shape &other) override;
	void intersect_impl(point &other) override;
	void intersect_impl(rectangle &other) override;
	void intersect_impl(circle &other) override;
};

struct rectangle: shape {
	rectangle(double x, double y, double width, double height)
		: shape(x, y)
		, width_(width)
		, height_(height)
	{}
	~rectangle() {}

	void intersect(shape &other) override;
	void intersect_impl(point &other) override;
	void intersect_impl(rectangle &other) override;
	void intersect_impl(circle &other) override;
private:
	double width_;
	double height_;
};

struct circle: shape {
	circle(double x, double y, double radius)
		: shape(x, y)
		, radius_(radius)
	{}
	~circle() {}

	void intersect(shape &other) override;
	void intersect_impl(point &other) override;
	void intersect_impl(rectangle &other) override;
	void intersect_impl(circle &other) override;
private:
	double radius_;
};

/**
 * Multimethod
 */
void intersect(shape &a, shape& b);