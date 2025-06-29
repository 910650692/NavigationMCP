package com.sgm.navi.service.define.navi;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.route.RouteChargeStationDetailInfo;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FyChargingStation implements Parcelable {
    // 电动汽车的能量消耗总量按行程点顺序排列 如:充电站1、充电站2、单位百分之一wh，有符号
    private long chargeEnrgySum;
    private RouteChargeStationDetailInfo chargeInfo;

    protected FyChargingStation(Parcel in) {
        chargeEnrgySum = in.readLong();
    }

    public static final Creator<FyChargingStation> CREATOR = new Creator<FyChargingStation>() {
        @Override
        public FyChargingStation createFromParcel(Parcel in) {
            return new FyChargingStation(in);
        }

        @Override
        public FyChargingStation[] newArray(int size) {
            return new FyChargingStation[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeLong(chargeEnrgySum);
    }
}
