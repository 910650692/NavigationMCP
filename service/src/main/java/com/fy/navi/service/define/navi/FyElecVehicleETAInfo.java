package com.fy.navi.service.define.navi;

import java.util.ArrayList;

/**
 * Author: QiuYaWei
 * Date: 2025/3/11
 * Description: [电动车ETA透出]
 */
public class FyElecVehicleETAInfo {
    public boolean energyEndFlag; // 是否有电量耗尽点，true 有
    public long pathID; // 该路况条对应的路线ID
    public ArrayList<Long> energySum; // 电动汽车的能量消耗总量按到达行程点的先后顺序排列， 如:途径地1、途径地2、目的地
}
