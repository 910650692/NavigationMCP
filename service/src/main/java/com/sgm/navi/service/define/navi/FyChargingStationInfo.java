package com.sgm.navi.service.define.navi;

import android.os.Parcel;
import android.os.Parcelable;


import androidx.annotation.NonNull;

import lombok.Getter;
import lombok.Setter;

/**
 * 充电站数据.
 */
@Getter
@Setter
public class FyChargingStationInfo implements Parcelable {
    /***
     * 是否支持地锁 @range [ true: 支持 false: 不支持 ] @default false
     */
    private boolean isSupportLock;
    /**
     * 是否支持提前预约 @range [ true: 支持 false: 不支持 ] @default false
     */
    private boolean isSupportOrder;
    /**
     * 动态慢充空闲个数 @unit 个 @range [ 空字符串: 服务没有下发数据 其他：动态慢充空闲个数 ] @default 空字符串.
     */
    private String slow_free;
    /**
     * 动态快充空闲个数 @unit 个 @range [ 空字符串: 服务没有下发数据 其他：动态快充空闲个数 ] @default 空字符串.
     */
    private String fast_free;
    /**
     * 动态慢充总个数 @unit 个 @range [ 空字符串: 服务没有下发数据 其他：动态慢充总个数 ] @default 空字符串.
     */
    private String slow_total;
    /**
     * 动态快充总个数 @unit 个 @range [ 空字符串: 服务没有下发数据 其他：动态快充总个数 ] @default 空字符串.
     */
    private String fast_total;
    /**
     * 充电站服务类型信息 @range [ 0:未知 1:公共充电站 2:品牌专用 3:出租充电站 4:公交(专用) 5:物流（专用） ].
     */
    private String cscf;
    /**
     * 静态慢充总个数.
     */
    private String num_slow;
    /**
     * 静态快充总个数.
     */
    private String num_fast;
    /**
     * 充电价格 @unit 元/度.
     */
    private String current_ele_price;
    /**
     * 服务价格 @unit 元/度.
     */
    private String current_ser_price;

    public FyChargingStationInfo() {
        this.slow_free = "";
        this.fast_free = "";
        this.slow_total = "";
        this.fast_total = "";
        this.cscf = "";
        this.num_slow = "";
        this.num_fast = "";
        this.current_ele_price = "";
        this.current_ser_price = "";
    }

    protected FyChargingStationInfo(Parcel in) {
        isSupportLock = in.readByte() != 0;
        isSupportOrder = in.readByte() != 0;
        slow_free = in.readString();
        fast_free = in.readString();
        slow_total = in.readString();
        fast_total = in.readString();
        cscf = in.readString();
        num_slow = in.readString();
        num_fast = in.readString();
        current_ele_price = in.readString();
        current_ser_price = in.readString();
    }

    public static final Creator<FyChargingStationInfo> CREATOR = new Creator<FyChargingStationInfo>() {
        @Override
        public FyChargingStationInfo createFromParcel(Parcel in) {
            return new FyChargingStationInfo(in);
        }

        @Override
        public FyChargingStationInfo[] newArray(int size) {
            return new FyChargingStationInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeByte((byte) (isSupportLock ? 1 : 0));
        dest.writeByte((byte) (isSupportOrder ? 1 : 0));
        dest.writeString(slow_free);
        dest.writeString(fast_free);
        dest.writeString(slow_total);
        dest.writeString(fast_total);
        dest.writeString(cscf);
        dest.writeString(num_slow);
        dest.writeString(num_fast);
        dest.writeString(current_ele_price);
        dest.writeString(current_ser_price);
    }
}
