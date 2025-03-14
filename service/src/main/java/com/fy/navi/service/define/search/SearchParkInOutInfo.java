package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: 停车场出入口
 * @CreateDate: 2025/3/10 15:02
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchParkInOutInfo implements Parcelable {
    private double x;
    private double y;
    private String keytype;
    private String entExitId;

    protected SearchParkInOutInfo(Parcel in) {
        x = in.readDouble();
        y = in.readDouble();
        keytype = in.readString();
        entExitId = in.readString();
    }

    public static final Creator<SearchParkInOutInfo> CREATOR = new Creator<SearchParkInOutInfo>() {
        @Override
        public SearchParkInOutInfo createFromParcel(Parcel in) {
            return new SearchParkInOutInfo(in);
        }

        @Override
        public SearchParkInOutInfo[] newArray(int size) {
            return new SearchParkInOutInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeDouble(x);
        parcel.writeDouble(y);
        parcel.writeString(keytype);
        parcel.writeString(entExitId);
    }
}
