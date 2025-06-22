package com.sgm.navi.service.define.hotupdate;

import androidx.annotation.NonNull;

public class MapNumInfo {

    private String mStrKey;
    private String mStrVersion;
    private String mStrContent;

    public MapNumInfo() {
        this.mStrKey = "";
        this.mStrVersion = "";
        this.mStrContent = "";
    }

    public MapNumInfo(final String strKeyLiteObj, final String strVersionLiteObj, final String strContentLiteObj) {
        this.mStrKey = strKeyLiteObj;
        this.mStrVersion = strVersionLiteObj;
        this.mStrContent = strContentLiteObj;
    }

    public String getStrKey() {
        return mStrKey;
    }

    public void setStrKey(final String strKey) {
        this.mStrKey = strKey;
    }

    public String getStrVersion() {
        return mStrVersion;
    }

    public void setStrVersion(final String strVersion) {
        this.mStrVersion = strVersion;
    }

    public String getStrContent() {
        return mStrContent;
    }

    public void setStrContent(final String strContent) {
        this.mStrContent = strContent;
    }

    @NonNull
    @Override
    public String toString() {
        return "MapNumInfo{" +
                "strKey='" + mStrKey + '\'' +
                ", strVersion='" + mStrVersion + '\'' +
                ", strContent='" + mStrContent + '\'' +
                '}';
    }
}
