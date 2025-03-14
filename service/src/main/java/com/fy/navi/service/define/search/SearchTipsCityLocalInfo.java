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
public class SearchTipsCityLocalInfo implements Parcelable {
    public int adcode;
    public int index;
    public String name;

    protected SearchTipsCityLocalInfo(Parcel in) {
        adcode = in.readInt();
        index = in.readInt();
        name = in.readString();
    }

    public static final Creator<SearchTipsCityLocalInfo> CREATOR = new Creator<SearchTipsCityLocalInfo>() {
        @Override
        public SearchTipsCityLocalInfo createFromParcel(Parcel in) {
            return new SearchTipsCityLocalInfo(in);
        }

        @Override
        public SearchTipsCityLocalInfo[] newArray(int size) {
            return new SearchTipsCityLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(adcode);
        parcel.writeInt(index);
        parcel.writeString(name);
    }
}
