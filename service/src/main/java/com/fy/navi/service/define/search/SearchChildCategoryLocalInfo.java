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
public class SearchChildCategoryLocalInfo implements Parcelable {
    //选中状态
    public int checked;
    //名称
    public String name;
    //请求参数值
    public String value;
    //子类目信息
    public List<SearchChildCategoryLocalInfo> categoryLocalInfos;

    protected SearchChildCategoryLocalInfo(Parcel in) {
        name = in.readString();
        value = in.readString();
        checked = in.readInt();
        categoryLocalInfos = in.createTypedArrayList(SearchChildCategoryLocalInfo.CREATOR);
    }

    public static final Creator<SearchChildCategoryLocalInfo> CREATOR = new Creator<SearchChildCategoryLocalInfo>() {
        @Override
        public SearchChildCategoryLocalInfo createFromParcel(Parcel in) {
            return new SearchChildCategoryLocalInfo(in);
        }

        @Override
        public SearchChildCategoryLocalInfo[] newArray(int size) {
            return new SearchChildCategoryLocalInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(name);
        parcel.writeString(value);
        parcel.writeInt(checked);
        parcel.writeTypedList(categoryLocalInfos);
    }
}
