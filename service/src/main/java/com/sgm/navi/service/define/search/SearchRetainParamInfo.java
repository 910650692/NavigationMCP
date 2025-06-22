package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.autonavi.gbl.search.model.SearchTipsInfo;

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
public class SearchRetainParamInfo implements Parcelable {
    private String mKeywordBizType;

    private SearchTipsLocalInfo mSearchTipsLocalInfo;

    public String getKeywordBizType() {
        return mKeywordBizType;
    }

    /**
     * 搜索业务类型（美食，洗车等）
     * @param keywordBizType 搜索业务类型
     * @return SearchRetainParamInfo
     */
    public SearchRetainParamInfo setKeywordBizType(final String keywordBizType) {
        this.mKeywordBizType = keywordBizType;
        return this;
    }

    public SearchTipsLocalInfo getSearchTipsLocalInfo() {
        return mSearchTipsLocalInfo;
    }

    /**
     * 搜索提示参数（如关键字纠错、城市建议等）
     * @param searchTipsLocalInfo 搜索提示参数
     * @return SearchRetainParamInfo
     */
    public SearchRetainParamInfo setSearchTipsLocalInfo(final SearchTipsLocalInfo searchTipsLocalInfo) {
        this.mSearchTipsLocalInfo = searchTipsLocalInfo;
        return this;
    }

    protected SearchRetainParamInfo(final Parcel in) {
        mKeywordBizType = in.readString();
        mSearchTipsLocalInfo = in.readParcelable(SearchTipsInfo.class.getClassLoader());
    }

    public static final Creator<SearchRetainParamInfo> CREATOR = new Creator<SearchRetainParamInfo>() {
        @Override
        public SearchRetainParamInfo createFromParcel(final Parcel in) {
            return new SearchRetainParamInfo(in);
        }

        @Override
        public SearchRetainParamInfo[] newArray(final int size) {
            return new SearchRetainParamInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeString(mKeywordBizType);
        parcel.writeParcelable(mSearchTipsLocalInfo, i);
    }
}
