package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChargeEquipmentInfo implements Parcelable {
    // 设备编号
    private String equipmentId;
    // 国家标准
    private String nationalStandard;
    // 额定功率
    private String power;
    // 充电类型
    private int chargeType;
    // 车位号
    private String parkNo;
    // 接口状态 0:离网1：空闲2：占用（未充电）3：占用（充电中）4：占用（已预约）255（故障）
    private int status;
    // 是否存在地锁
    private int parkingLockFlag;


    public ChargeEquipmentInfo(Parcel in) {
        equipmentId = in.readString();
        nationalStandard = in.readString();
        power = in.readString();
        chargeType = in.readInt();
        parkNo = in.readString();
        status = in.readInt();
        parkingLockFlag = in.readInt();
    }

    public static final Creator<ChargeEquipmentInfo> CREATOR = new Creator<ChargeEquipmentInfo>() {
        @Override
        public ChargeEquipmentInfo createFromParcel(Parcel in) {
            return new ChargeEquipmentInfo(in);
        }

        @Override
        public ChargeEquipmentInfo[] newArray(int size) {
            return new ChargeEquipmentInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(equipmentId);
        parcel.writeString(nationalStandard);
        parcel.writeString(power);
        parcel.writeInt(chargeType);
        parcel.writeString(parkNo);
        parcel.writeInt(status);
        parcel.writeInt(parkingLockFlag);
    }


    public String getEquipmentId() {
        return equipmentId;
    }

    public ChargeEquipmentInfo setEquipmentId(String equipmentId) {
        this.equipmentId = equipmentId;
        return this;
    }

    public String getNationalStandard() {
        return nationalStandard;
    }

    public ChargeEquipmentInfo setNationalStandard(String nationalStandard) {
        this.nationalStandard = nationalStandard;
        return this;
    }

    public String getPower() {
        return power;
    }

    public ChargeEquipmentInfo setPower(String power) {
        this.power = power;
        return this;
    }

    public int getChargeType() {
        return chargeType;
    }

    public ChargeEquipmentInfo setChargeType(int chargeType) {
        this.chargeType = chargeType;
        return this;
    }

    public String getParkNo() {
        return parkNo;
    }

    public ChargeEquipmentInfo setParkNo(String parkNo) {
        this.parkNo = parkNo;
        return this;
    }

    public int getStatus() {
        return status;
    }

    public ChargeEquipmentInfo setStatus(int status) {
        this.status = status;
        return this;
    }

    public int getParkingLockFlag() {
        return parkingLockFlag;
    }

    public ChargeEquipmentInfo setParkingLockFlag(int parkingLockFlag) {
        this.parkingLockFlag = parkingLockFlag;
        return this;
    }
}
