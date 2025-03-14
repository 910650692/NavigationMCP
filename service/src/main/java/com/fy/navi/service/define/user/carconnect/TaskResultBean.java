package com.fy.navi.service.define.user.carconnect;

public class TaskResultBean {
    //错误码
    private int errorCode;
    //错误信息
    private String errorMessage;
    //请求任务ID
    private long taskId;

    public int getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(int errorCode) {
        this.errorCode = errorCode;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public long getTaskId() {
        return taskId;
    }

    public void setTaskId(long taskId) {
        this.taskId = taskId;
    }
}
