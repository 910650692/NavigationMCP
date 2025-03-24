package com.fy.navi.service.define.search;

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
public class SearchChildCategoryLocalInfo implements Parcelable {
    //选中状态
    private int mChecked;
    //名称
    private String mName;
    //请求参数值
    private String mValue;
    //子类目信息
    private List<SearchChildCategoryLocalInfo> mCategoryLocalInfos;

    public int getChecked() {
        return mChecked;
    }

    /**
     * 设置选中状态
     * @param checked 选中状态
     * @return SearchChildCategoryLocalInfo
     */
    public SearchChildCategoryLocalInfo setChecked(final int checked) {
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
    public SearchChildCategoryLocalInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public List<SearchChildCategoryLocalInfo> getCategoryLocalInfos() {
        return mCategoryLocalInfos;
    }

    /**
     * 设置子类目信息
     * @param categoryLocalInfos 子类目信息
     * @return SearchChildCategoryLocalInfo
     */
    public SearchChildCategoryLocalInfo setCategoryLocalInfos(final List<SearchChildCategoryLocalInfo> categoryLocalInfos) {
        this.mCategoryLocalInfos = categoryLocalInfos;
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
    public SearchChildCategoryLocalInfo setValue(final String value) {
        this.mValue = value;
        return this;
    }

    protected SearchChildCategoryLocalInfo(final Parcel in) {
        mName = in.readString();
        mValue = in.readString();
        mChecked = in.readInt();
        mCategoryLocalInfos = in.createTypedArrayList(SearchChildCategoryLocalInfo.CREATOR);
    }

    public static final Creator<SearchChildCategoryLocalInfo> CREATOR = new Creator<SearchChildCategoryLocalInfo>() {
        @Override
        public SearchChildCategoryLocalInfo createFromParcel(final Parcel in) {
            return new SearchChildCategoryLocalInfo(in);
        }

        @Override
        public SearchChildCategoryLocalInfo[] newArray(final int size) {
            return new SearchChildCategoryLocalInfo[size];
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
        parcel.writeTypedList(mCategoryLocalInfos);
    }
}
