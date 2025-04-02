package com.fy.navi.service.define.user.carconnect;

public class TaskResultBean {
    //错误码
    private int mErrorCode;
    //错误信息
    private String mErrorMessage;
    //请求任务ID
    private long mTaskId;

    public int getErrorCode() {
        return mErrorCode;
    }

    public void setErrorCode(int errorCode) {
        this.mErrorCode = errorCode;
    }

    public String getErrorMessage() {
        return mErrorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.mErrorMessage = errorMessage;
    }

    public long getTaskId() {
        return mTaskId;
    }

    public void setTaskId(long taskId) {
        this.mTaskId = taskId;
    }
}
