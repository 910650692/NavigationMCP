package com.fy.navi.service.define.navi;

import java.util.ArrayList;

/**
 * Author: QiuYaWei
 * Date: 2025/3/11
 * Description: [电动车ETA透出]
 * @author fy
 * @version $Revision.*$
 */
public class FyElecVehicleETAInfo {
    private boolean mEnergyEndFlag; // 是否有电量耗尽点，true 有
    private long mPathID; // 该路况条对应的路线ID
    private ArrayList<Long> mEnergySum; // 电动汽车的能量消耗总量按到达行程点的先后顺序排列， 如:途径地1、途径地2、目的地

    public boolean getEnergyEndFlag() {
        return mEnergyEndFlag;
    }

    public void setEnergyEndFlag(final boolean energyEndFlag) {
        this.mEnergyEndFlag = energyEndFlag;
    }

    public long getPathID() {
        return mPathID;
    }

    public void setPathID(final long pathID) {
        this.mPathID = pathID;
    }

    public ArrayList<Long> getEnergySum() {
        return mEnergySum;
    }

    public void setEnergySum(final ArrayList<Long> energySum) {
        this.mEnergySum = energySum;
    }
}
