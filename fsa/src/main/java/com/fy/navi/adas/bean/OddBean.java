package com.fy.navi.adas.bean;

public class OddBean {
    private int error_code;
    private String error_message;
    private OddResponse response;

    public int getError_code() {
        return error_code;
    }

    public void setError_code(int error_code) {
        this.error_code = error_code;
    }

    public String getError_message() {
        return error_message;
    }

    public void setError_message(String error_message) {
        this.error_message = error_message;
    }

    public OddResponse getResponse() {
        return response;
    }

    public void setResponse(OddResponse response) {
        this.response = response;
    }
}
