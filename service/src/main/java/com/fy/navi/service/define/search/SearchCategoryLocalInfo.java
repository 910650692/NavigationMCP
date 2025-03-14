package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchCategoryLocalInfo implements Parcelable {
    //选中状态
    public int checked;
    //名称
    public String name;
    //子类目信息
    public List<SearchChildCategoryLocalInfo> categoryLocalInfos;

    protected SearchCategoryLocalInfo(Parcel in) {
        checked = in.readInt();
        name = in.readString();
        categoryLocalInfos = in.createTypedArrayList(SearchChildCategoryLocalInfo.CREATOR);
    }

    public static final Creator<SearchCategoryLocalInfo> CREATOR = new Creator<SearchCategoryLocalInfo>() {
        @Override
        public SearchCategoryLocalInfo createFromParcel(Parcel in) {
            return new SearchCategoryLocalInfo(in);
        }

        @Override
        public SearchCategoryLocalInfo[] newArray(int size) {
            return new SearchCategoryLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        checked = parcel.readInt();
        name = parcel.readString();
        categoryLocalInfos = parcel.createTypedArrayList(SearchChildCategoryLocalInfo.CREATOR);
    }
}
