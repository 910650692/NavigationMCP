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
public class SearchGrandChildCategoryLocalInfo implements Parcelable {
    //选中状态
    private int mChecked;
    //名称
    private String mName;
    //请求参数值
    private String mValue;
    public int getChecked() {
        return mChecked;
    }

    /**
     * 设置选中状态
     * @param checked 选中状态
     * @return SearchChildCategoryLocalInfo
     */
    public SearchGrandChildCategoryLocalInfo setChecked(final int checked) {
        this.mChecked = checked;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return SearchChildCategoryLocalInfo
     */
    public SearchGrandChildCategoryLocalInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getValue() {
        return mValue;
    }

    /**
     * 设置请求参数值
     * @param value 请求参数值
     * @return SearchChildCategoryLocalInfo
     */
    public SearchGrandChildCategoryLocalInfo setValue(final String value) {
        this.mValue = value;
        return this;
    }

    protected SearchGrandChildCategoryLocalInfo(final Parcel in) {
        mName = in.readString();
        mValue = in.readString();
        mChecked = in.readInt();
    }

    public static final Creator<SearchGrandChildCategoryLocalInfo> CREATOR = new Creator<SearchGrandChildCategoryLocalInfo>() {
        @Override
        public SearchGrandChildCategoryLocalInfo createFromParcel(final Parcel in) {
            return new SearchGrandChildCategoryLocalInfo(in);
        }

        @Override
        public SearchGrandChildCategoryLocalInfo[] newArray(final int size) {
            return new SearchGrandChildCategoryLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeString(mName);
        parcel.writeString(mValue);
        parcel.writeInt(mChecked);
    }
}
