package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.autonavi.gbl.search.model.SearchTipsCityInfo;
import com.autonavi.gbl.search.model.SearchTipsInfo;

import java.util.ArrayList;
import java.util.List;

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
public class SearchTipsLocalInfo implements Parcelable {
    public int sceneType;
    public String cityName;
    public String keyword;
    public ArrayList<String> optionalKeywordList;
    public List<SearchTipsCityLocalInfo> optionalCityList;
    public String retainState;
    protected SearchTipsLocalInfo(Parcel in) {
        sceneType = in.readInt();
        cityName = in.readString();
        keyword = in.readString();
        optionalKeywordList = in.createStringArrayList();
        optionalCityList = in.createTypedArrayList(SearchTipsCityLocalInfo.CREATOR);
        retainState = in.readString();
    }

    public static final Creator<SearchTipsLocalInfo> CREATOR = new Creator<SearchTipsLocalInfo>() {
        @Override
        public SearchTipsLocalInfo createFromParcel(Parcel in) {
            return new SearchTipsLocalInfo(in);
        }

        @Override
        public SearchTipsLocalInfo[] newArray(int size) {
            return new SearchTipsLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(sceneType);
        parcel.writeString(cityName);
        parcel.writeString(keyword);
        parcel.writeStringList(optionalKeywordList);
        parcel.writeTypedList(optionalCityList);
    }
}
