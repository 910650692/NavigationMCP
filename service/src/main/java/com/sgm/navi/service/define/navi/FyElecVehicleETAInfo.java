package com.sgm.navi.service.define.navi;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * Author: QiuYaWei
 * Date: 2025/3/11
 * Description: [电动车ETA透出]
 * @author sgm
 * @version $Revision.*$
 */
@Getter
@Setter
public class FyElecVehicleETAInfo {
    private boolean energyEndFlag; // 是否有电量耗尽点，true 有
    private long pathID; // 该路况条对应的路线ID
    private ArrayList<Integer> elecLinkConsume;//未走过link能耗 百分之一wh，有符号
    private ArrayList<Long> energySum; // 电动汽车的能量消耗总量按到达行程点的先后顺序排列， 如:途径地1、途径地2、目的地
    private ArrayList<FyChargingStation> chargeStationInfo;
    private ArrayList<FyViaMergeInfo> viaMergeInfo;
    private FyEnergyEndPoint energyEndPoint;//能量耗尽点

    @Override
    public String toString() {
        return "FyElecVehicleETAInfo{" +
                "energyEndFlag=" + energyEndFlag +
                ", pathID=" + pathID +
                ", elecLinkConsume=" + elecLinkConsume +
                ", energySum=" + energySum +
                ", chargeStationInfo=" + chargeStationInfo +
                ", viaMergeInfo=" + viaMergeInfo +
                ", energyEndPoint=" + energyEndPoint +
                '}';
    }
}
