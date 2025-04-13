package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

public class SuggestChangePathReasonEntity {

    private long mSaveTime;

    public long getSaveTime() {
        return mSaveTime;
    }

    public void setSaveTime(final long saveTime) {
        this.mSaveTime = saveTime;
    }

    @NonNull
    @Override
    public String toString() {
        return "onSuggestChangePathEntity{" +
                "saveTime=" + mSaveTime +
                '}';
    }
}
