package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author yufei.cai
 * @version \$Revision1.0\$
 * @Description: SearchRetainParamInfo: 服务状态回传参数，来自于关键字搜索结果
 * @CreateDate: 2025/2/21 14:55
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchTipsCityLocalInfo implements Parcelable {
    private int mAdcode;
    private int mIndex;
    private String mName;

    public int getAdcode() {
        return mAdcode;
    }

    /**
     * 设置城市编码
     * @param adcode 城市编码
     * @return SearchTipsCityLocalInfo
     */
    public SearchTipsCityLocalInfo setAdcode(final int adcode) {
        this.mAdcode = adcode;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置城市名称
     * @param name 城市名称
     * @return SearchTipsCityLocalInfo
     */
    public SearchTipsCityLocalInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public int getIndex() {
        return mIndex;
    }

    /**
     * 设置索引
     * @param index 索引
     * @return SearchTipsCityLocalInfo
     */
    public SearchTipsCityLocalInfo setIndex(final int index) {
        this.mIndex = index;
        return this;
    }

    protected SearchTipsCityLocalInfo(final Parcel in) {
        mAdcode = in.readInt();
        mIndex = in.readInt();
        mName = in.readString();
    }

    public static final Creator<SearchTipsCityLocalInfo> CREATOR = new Creator<SearchTipsCityLocalInfo>() {
        @Override
        public SearchTipsCityLocalInfo createFromParcel(final Parcel in) {
            return new SearchTipsCityLocalInfo(in);
        }

        @Override
        public SearchTipsCityLocalInfo[] newArray(final int size) {
            return new SearchTipsCityLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mAdcode);
        parcel.writeInt(mIndex);
        parcel.writeString(mName);
    }
}
