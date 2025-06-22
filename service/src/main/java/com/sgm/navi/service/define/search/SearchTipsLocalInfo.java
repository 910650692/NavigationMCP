package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

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
public class SearchTipsLocalInfo implements Parcelable {
    private int mSceneType;
    private String mCityName;
    private String mKeyword;
    private ArrayList<String> mOptionalKeywordList;
    private List<SearchTipsCityLocalInfo> mOptionalCityList;
    private String mRetainState;

    public int getSceneType() {
        return mSceneType;
    }

    /**
     * 设置场景类型
     * @param sceneType 场景类型
     * @return SearchTipsLocalInfo
     */
    public SearchTipsLocalInfo setSceneType(final int sceneType) {
        this.mSceneType = sceneType;
        return this;
    }

    public String getCityName() {
        return mCityName;
    }

    /**
     * 设置城市名称
     * @param cityName 城市名称
     * @return SearchTipsLocalInfo
     */
    public SearchTipsLocalInfo setCityName(final String cityName) {
        this.mCityName = cityName;
        return this;
    }

    public String getKeyword() {
        return mKeyword;
    }

    /**
     * 设置关键字
     * @param keyword 关键字
     * @return SearchTipsLocalInfo
     */
    public SearchTipsLocalInfo setKeyword(final String keyword) {
        this.mKeyword = keyword;
        return this;
    }

    public ArrayList<String> getOptionalKeywordList() {
        return mOptionalKeywordList;
    }

    /**
     * 设置可选关键字列表
     * @param optionalKeywordList 可选关键字列表
     * @return SearchTipsLocalInfo
     */
    public SearchTipsLocalInfo setOptionalKeywordList(final ArrayList<String> optionalKeywordList) {
        this.mOptionalKeywordList = optionalKeywordList;
        return this;
    }

    public List<SearchTipsCityLocalInfo> getOptionalCityList() {
        return mOptionalCityList;
    }

    /**
     * 设置可选城市列表
     * @param optionalCityList 可选城市列表
     * @return SearchTipsLocalInfo
     */
    public SearchTipsLocalInfo setOptionalCityList(final List<SearchTipsCityLocalInfo> optionalCityList) {
        this.mOptionalCityList = optionalCityList;
        return this;
    }

    public String getRetainState() {
        return mRetainState;
    }

    /**
     * 设置回传参数
     * @param retainState 回传参数
     * @return SearchTipsLocalInfo
     */
    public SearchTipsLocalInfo setRetainState(final String retainState) {
        this.mRetainState = retainState;
        return this;
    }

    protected SearchTipsLocalInfo(final Parcel in) {
        mSceneType = in.readInt();
        mCityName = in.readString();
        mKeyword = in.readString();
        mOptionalKeywordList = in.createStringArrayList();
        mOptionalCityList = in.createTypedArrayList(SearchTipsCityLocalInfo.CREATOR);
        mRetainState = in.readString();
    }

    public static final Creator<SearchTipsLocalInfo> CREATOR = new Creator<SearchTipsLocalInfo>() {
        @Override
        public SearchTipsLocalInfo createFromParcel(final Parcel in) {
            return new SearchTipsLocalInfo(in);
        }

        @Override
        public SearchTipsLocalInfo[] newArray(final int size) {
            return new SearchTipsLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mSceneType);
        parcel.writeString(mCityName);
        parcel.writeString(mKeyword);
        parcel.writeStringList(mOptionalKeywordList);
        parcel.writeTypedList(mOptionalCityList);
    }
}
