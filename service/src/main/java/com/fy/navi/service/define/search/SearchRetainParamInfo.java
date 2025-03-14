package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.autonavi.gbl.search.model.SearchTipsInfo;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: yufei.cai
 * @Description: SearchRetainParamInfo: 服务状态回传参数，来自于关键字搜索结果
 * @CreateDate: 2025/2/21 14:55
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchRetainParamInfo implements Parcelable {
    public String keywordBizType;

    public SearchTipsLocalInfo searchTipsLocalInfo;

    protected SearchRetainParamInfo(Parcel in) {
        keywordBizType = in.readString();
        searchTipsLocalInfo = in.readParcelable(SearchTipsInfo.class.getClassLoader());
    }

    public static final Creator<SearchRetainParamInfo> CREATOR = new Creator<SearchRetainParamInfo>() {
        @Override
        public SearchRetainParamInfo createFromParcel(Parcel in) {
            return new SearchRetainParamInfo(in);
        }

        @Override
        public SearchRetainParamInfo[] newArray(int size) {
            return new SearchRetainParamInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(keywordBizType);
        parcel.writeParcelable(searchTipsLocalInfo, i);
    }
}
