package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.ArrayList;

public class BaseSearchResult implements Parcelable {

    private int mCode;
    private int maxPageNum;
    private ArrayList<BaseSearchPoi> mPoiList = new ArrayList<>();

    public BaseSearchResult() {

    }

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
        mCode = in.readInt();
        maxPageNum = in.readInt();
        mPoiList = in.createTypedArrayList(BaseSearchPoi.CREATOR);
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeInt(mCode);
        dest.writeInt(maxPageNum);
        dest.writeTypedList(mPoiList);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public int getCode() {
        return mCode;
    }

    public void setCode(int code) {
        this.mCode = code;
    }

    public int getMaxPageNum() {
        return maxPageNum;
    }

    public void setMaxPageNum(int maxPageNum) {
        this.maxPageNum = maxPageNum;
    }

    public ArrayList<BaseSearchPoi> getPoiList() {
        return mPoiList;
    }

    public void setPoiList(ArrayList<BaseSearchPoi> poiList) {
        this.mPoiList = poiList;
    }
}
