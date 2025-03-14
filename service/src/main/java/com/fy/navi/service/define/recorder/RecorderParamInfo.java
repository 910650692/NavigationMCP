package com.fy.navi.service.define.recorder;

public class RecorderParamInfo {
    /**
     * 是否自动删除
     */
    private boolean autoDelete;
    /**
     * 单个文件大小(单位为m,至少需要为5m,最多30m)，超过设定值则存储新文件
     */
    private int maxFileSize;
    /**
     * 至多保留文件数(单位为个数，至少需要为1个)
     */
    private int maxFiles;
    /**
     * 录制文件路径
     */
    private String recordPath;

    public boolean isAutoDelete() {
        return autoDelete;
    }

    public void setAutoDelete(boolean autoDelete) {
        this.autoDelete = autoDelete;
    }

    public int getMaxFileSize() {
        return maxFileSize;
    }

    public void setMaxFileSize(int maxFileSize) {
        this.maxFileSize = maxFileSize;
    }

    public int getMaxFiles() {
        return maxFiles;
    }

    public void setMaxFiles(int maxFiles) {
        this.maxFiles = maxFiles;
    }

    public String getRecordPath() {
        return recordPath;
    }

    public void setRecordPath(String recordPath) {
        this.recordPath = recordPath;
    }
}
