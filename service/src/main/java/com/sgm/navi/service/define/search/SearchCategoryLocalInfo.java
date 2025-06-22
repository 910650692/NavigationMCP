package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;


import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchCategoryLocalInfo implements Parcelable {
    //选中状态
    private int mChecked;
    //名称
    private String mName;
    //子类目信息
    private List<SearchChildCategoryLocalInfo> mCategoryLocalInfos;

    public int getChecked() {
        return mChecked;
    }

    /**
     * 设置选中状态
     * @param checked 选中状态
     * @return SearchCategoryLocalInfo
     */
    public SearchCategoryLocalInfo setChecked(final int checked) {
        this.mChecked = checked;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return SearchCategoryLocalInfo
     */
    public SearchCategoryLocalInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public List<SearchChildCategoryLocalInfo> getCategoryLocalInfos() {
        return mCategoryLocalInfos;
    }

    /**
     * 设置子类目信息
     * @param categoryLocalInfos 子类目信息
     * @return SearchCategoryLocalInfo
     */
    public SearchCategoryLocalInfo setCategoryLocalInfos(final List<SearchChildCategoryLocalInfo> categoryLocalInfos) {
        this.mCategoryLocalInfos = categoryLocalInfos;
        return this;
    }

    protected SearchCategoryLocalInfo(final Parcel in) {
        mChecked = in.readInt();
        mName = in.readString();
        mCategoryLocalInfos = in.createTypedArrayList(SearchChildCategoryLocalInfo.CREATOR);
    }

    public static final Creator<SearchCategoryLocalInfo> CREATOR = new Creator<SearchCategoryLocalInfo>() {
        @Override
        public SearchCategoryLocalInfo createFromParcel(final Parcel in) {
            return new SearchCategoryLocalInfo(in);
        }

        @Override
        public SearchCategoryLocalInfo[] newArray(final int size) {
            return new SearchCategoryLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        mChecked = parcel.readInt();
        mName = parcel.readString();
        mCategoryLocalInfos = parcel.createTypedArrayList(SearchChildCategoryLocalInfo.CREATOR);
    }
}
