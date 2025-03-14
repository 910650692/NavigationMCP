package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.ArrayList;

public class BaseSearchResult implements Parcelable {

    private int code;
    private int maxPageNum;
    private ArrayList<BaseSearchPoi> poiList = new ArrayList<>();

    public BaseSearchResult() {}

    public static final Creator<BaseSearchResult> CREATOR = new Creator<BaseSearchResult>() {
        @Override
        public BaseSearchResult createFromParcel(Parcel source) {
            return new BaseSearchResult(source);
        }

        @Override
        public BaseSearchResult[] newArray(int size) {
            return new BaseSearchResult[size];
        }
    };

    public BaseSearchResult(Parcel in) {
        code = in.readInt();
        maxPageNum = in.readInt();
        poiList = in.createTypedArrayList(BaseSearchPoi.CREATOR);
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeInt(code);
        dest.writeInt(maxPageNum);
        dest.writeTypedList(poiList);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public int getMaxPageNum() {
        return maxPageNum;
    }

    public void setMaxPageNum(int maxPageNum) {
        this.maxPageNum = maxPageNum;
    }

    public ArrayList<BaseSearchPoi> getPoiList() {
        return poiList;
    }

    public void setPoiList(ArrayList<BaseSearchPoi> poiList) {
        this.poiList = poiList;
    }
}
