//
// Created by egorbunov on 10/18/15.
//

#ifndef RESULT_H_INCLUDED__
#define RESULT_H_INCLUDED__

#include <string>

namespace {
    const std::string OK_STRING = "OK";
}

namespace au {
    template<typename T>
    class Result {
    public:

        static Result ok(T data) {
            Result result;
            result.okFlag = true;
            result.errMessage = OK_STRING;
            result.data = data;
            return result;
        }

        static Result error(std::string msg, T dummyVal) {
            Result result;
            result.okFlag = false;
            result.errMessage = msg;
            result.data = dummyVal;
            return result;
        }

        std::string msg() {
            return errMessage;
        }

        bool isOk() {
            return okFlag;
        }

        T getData() {
            return data;
        }

    private:
        Result() {
        }
        bool okFlag;
        std::string errMessage;
        T data;
    };


}

#endif //RESULT_H_INCLUDED__
