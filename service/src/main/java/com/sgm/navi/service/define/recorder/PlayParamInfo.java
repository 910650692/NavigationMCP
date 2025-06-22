package com.sgm.navi.service.define.recorder;

public class PlayParamInfo {

    /**
     * 是否循环播放
     */
    private boolean mIsLooping;
    /**
     * 回放文件路径(目录或文件)
     */
    private String mPlayPath;

    public boolean isLooping() {
        return mIsLooping;
    }

    public void setLooping(final boolean looping) {
        mIsLooping = looping;
    }

    public String getPlayPath() {
        return mPlayPath;
    }

    public void setPlayPath(final String playPath) {
        this.mPlayPath = playPath;
    }
}
