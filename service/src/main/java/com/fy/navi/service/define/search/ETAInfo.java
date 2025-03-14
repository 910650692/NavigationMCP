package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: ETAInfo:到达目的地信息
 * @CreateDate: 2025/2/12 13:33
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ETAInfo implements Parcelable {
    private int distance; //距离终点距离
    private String travelTime; //预计行程时间
    private int leftCharge; //剩余电量

    protected ETAInfo(Parcel in) {
       distance = in.readInt();
        travelTime = in.readString();
        leftCharge = in.readInt();
    }

    public static final Creator<ETAInfo> CREATOR = new Creator<ETAInfo>() {
        @Override
        public ETAInfo createFromParcel(Parcel in) {
            return new ETAInfo(in);
        }

        @Override
        public ETAInfo[] newArray(int size) {
            return new ETAInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(distance);
        parcel.writeString(travelTime);
        parcel.writeInt(leftCharge);
    }
}
