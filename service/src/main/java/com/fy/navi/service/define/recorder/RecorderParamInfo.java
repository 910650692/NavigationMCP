package com.fy.navi.service.define.recorder;

public class RecorderParamInfo {
    /**
     * 是否自动删除
     */
    private boolean mAutoDelete;
    /**
     * 单个文件大小(单位为m,至少需要为5m,最多30m)，超过设定值则存储新文件
     */
    private int mMaxFileSize;
    /**
     * 至多保留文件数(单位为个数，至少需要为1个)
     */
    private int mMaxFiles;
    /**
     * 录制文件路径
     */
    private String mRecordPath;

    public boolean isAutoDelete() {
        return mAutoDelete;
    }

    public void setAutoDelete(final boolean autoDelete) {
        this.mAutoDelete = autoDelete;
    }

    public int getMaxFileSize() {
        return mMaxFileSize;
    }

    public void setMaxFileSize(final int maxFileSize) {
        this.mMaxFileSize = maxFileSize;
    }

    public int getMaxFiles() {
        return mMaxFiles;
    }

    public void setMaxFiles(final int maxFiles) {
        this.mMaxFiles = maxFiles;
    }

    public String getRecordPath() {
        return mRecordPath;
    }

    public void setRecordPath(final String recordPath) {
        this.mRecordPath = recordPath;
    }
}
