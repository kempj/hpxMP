#include <stdlib.h>
#include <vector>

using std::vector;

struct block {
    block(const block& other) : ptr(other.ptr), stride(other.stride), width(other.width), height(other.height), data(other.stride * other.stride)
    {
        /*
        for(int i = 0; i < height; i++) {
            for(int j = 0; j < width; j++) {
                data[i*stride + j] = other[i][j];
            }
        }
        */
    }

    block(double *matrix, int size) {
        ptr = matrix;
        stride = size;
        height = size;
        width = size;
    }
    block(int size) : data(size*size)
    {
        stride = size;
        width = size;
        height = size;
        ptr = data.data();

        const int RM = 100;
        double rv1 = (rand() % RM) / ((double)RM);
        double rv2 = (rand() % RM) / ((double)RM);
        for (int i = 0; i < size; i++) {
            data[i] = ((i+1) * rv1) / ((i+1) * rv2);
        }
    }

    block() {}

    double *ptr;
    int stride{0};
    int row{0};
    int col{0};
    int width{0};
    int height{0};
    vector<double> data;

    double* operator[](int idx) const {
        return &(ptr[row*stride + col + idx]);
    }
    block block11(){
        block tmp;
        tmp.ptr = ptr;
        tmp.stride = stride;
        tmp.row = row;
        tmp.col = col;
        tmp.width = width/2;
        tmp.height = height/2;
        return tmp;
    }
    block block12(){
        block tmp;
        tmp.ptr = ptr;
        tmp.stride = stride;
        tmp.row = row;
        tmp.col = col + width/2;
        tmp.width = width - (width/2);
        tmp.height = height/2;
        return tmp;
    }
    block block21(){
        block tmp;
        tmp.ptr = ptr;
        tmp.stride = stride;
        tmp.row = row + height/2;
        tmp.col = col;
        tmp.width = width/2;
        tmp.height = height - (height/2);
        return tmp;
    }
    block block22(){
        block tmp;
        tmp.ptr = ptr;
        tmp.stride = stride;
        tmp.row = row + height/2;
        tmp.col = col + width/2;
        tmp.width = width - (width/2);
        tmp.height = height - (height/2);
        return tmp;
    }
};
