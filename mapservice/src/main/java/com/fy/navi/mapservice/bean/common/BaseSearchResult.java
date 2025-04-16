package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.ArrayList;

public class BaseSearchResult implements Parcelable {

    //GBL返回的错误码
    private int mCode;
    //搜索结果当前页码，从1开始
    private int mPageNum;
    //搜索结果最大页数
    private int maxPageNum;
    //搜索Poi列表
    private ArrayList<BaseSearchPoi> mPoiList = new ArrayList<>();

    public BaseSearchResult() {

    }

    public static final Creator<BaseSearchResult> CREATOR = new Creator<BaseSearchResult>() {
        @Override
        public BaseSearchResult createFromParcel(final Parcel source) {
            return new BaseSearchResult(source);
        }

        @Override
        public BaseSearchResult[] newArray(final int size) {
            return new BaseSearchResult[size];
        }
    };

    public BaseSearchResult(final Parcel in) {
        mCode = in.readInt();
        mPageNum = in.readInt();
        maxPageNum = in.readInt();
        mPoiList = in.createTypedArrayList(BaseSearchPoi.CREATOR);
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeInt(mCode);
        dest.writeInt(mPageNum);
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

    public void setCode(final int code) {
        this.mCode = code;
    }

    public int getPageNum() {
        return mPageNum;
    }

    public void setPageNum(final int pageNum) {
        mPageNum = pageNum;
    }

    public int getMaxPageNum() {
        return maxPageNum;
    }

    public void setMaxPageNum(final int maxPageNum) {
        this.maxPageNum = maxPageNum;
    }

    public ArrayList<BaseSearchPoi> getPoiList() {
        return mPoiList;
    }

    public void setPoiList(final ArrayList<BaseSearchPoi> poiList) {
        this.mPoiList = poiList;
    }
}
