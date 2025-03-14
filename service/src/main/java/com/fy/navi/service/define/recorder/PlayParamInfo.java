package com.fy.navi.service.define.recorder;

public class PlayParamInfo {

    /**
     * 是否循环播放
     */
    private boolean isLooping;
    /**
     * 回放文件路径(目录或文件)
     */
    private String playPath;

    public boolean isLooping() {
        return isLooping;
    }

    public void setLooping(boolean looping) {
        isLooping = looping;
    }

    public String getPlayPath() {
        return playPath;
    }

    public void setPlayPath(String playPath) {
        this.playPath = playPath;
    }
}
