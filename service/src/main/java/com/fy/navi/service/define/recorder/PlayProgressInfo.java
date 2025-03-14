package com.fy.navi.service.define.recorder;


public class PlayProgressInfo {

    private long fileIndex;
    private long fileTotalCount;
    private String playName;
    private long currentMessageIndex;
    private long totalMessageCount;
    private long unixTimestamp;

    public String getPlayName() {
        return playName;
    }

    public void setPlayName(String playName) {
        this.playName = playName;
    }

    public long getFileIndex() {
        return fileIndex;
    }

    public void setFileIndex(long fileIndex) {
        this.fileIndex = fileIndex;
    }

    public long getFileTotalCount() {
        return fileTotalCount;
    }

    public void setFileTotalCount(long fileTotalCount) {
        this.fileTotalCount = fileTotalCount;
    }

    public long getCurrentMessageIndex() {
        return currentMessageIndex;
    }

    public void setCurrentMessageIndex(long currentMessageIndex) {
        this.currentMessageIndex = currentMessageIndex;
    }

    public long getTotalMessageCount() {
        return totalMessageCount;
    }

    public void setTotalMessageCount(long totalMessageCount) {
        this.totalMessageCount = totalMessageCount;
    }

    public long getUnixTimestamp() {
        return unixTimestamp;
    }

    public void setUnixTimestamp(long unixTimestamp) {
        this.unixTimestamp = unixTimestamp;
    }


}
