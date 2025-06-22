package com.sgm.navi.service.define.recorder;


public class PlayProgressInfo {

    private long mFileIndex;
    private long mFileTotalCount;
    private String mPlayName;
    private long mCurrentMessageIndex;
    private long mTotalMessageCount;
    private long mUnixTimestamp;

    public String getPlayName() {
        return mPlayName;
    }

    public void setPlayName(final String playName) {
        this.mPlayName = playName;
    }

    public long getFileIndex() {
        return mFileIndex;
    }

    public void setFileIndex(final long fileIndex) {
        this.mFileIndex = fileIndex;
    }

    public long getFileTotalCount() {
        return mFileTotalCount;
    }

    public void setFileTotalCount(final long fileTotalCount) {
        this.mFileTotalCount = fileTotalCount;
    }

    public long getCurrentMessageIndex() {
        return mCurrentMessageIndex;
    }

    public void setCurrentMessageIndex(final long currentMessageIndex) {
        this.mCurrentMessageIndex = currentMessageIndex;
    }

    public long getTotalMessageCount() {
        return mTotalMessageCount;
    }

    public void setTotalMessageCount(final long totalMessageCount) {
        this.mTotalMessageCount = totalMessageCount;
    }

    public long getUnixTimestamp() {
        return mUnixTimestamp;
    }

    public void setUnixTimestamp(final long unixTimestamp) {
        this.mUnixTimestamp = unixTimestamp;
    }


}
