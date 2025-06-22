package com.sgm.navi.service.define.position;


import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

public class LocParallelInfoEntity {

    /**
     * 0: 非平行路切换期间 表示当前车辆未处于主辅路切换的状态
     * 1: 平行路切换期间 表示当前车辆正处于主辅路切换的状态
     * 该变量主要用于导航场景中，帮助开发者判断车辆是否在主辅路之间进行切换。结合其他成员变量（如 flag 和 hwFlag），
     * 可以更精确地定位车辆所在道路及其周边道路的属性。
     */
    @Setter
    @Getter
    private int status;

    /**
     * 0：无主辅路 表示车标所在道路旁没有主辅路
     * 1：车标在主路 表示车标所在道路为主路，并且该道路旁有辅路（可切换到辅路）
     * 2：车标在辅路 表示车标所在道路为辅路，并且该道路旁有主路（可切换到主路）
     * 在导航过程中，当系统检测到车辆位于主路或辅路时，可以通过 flag 的值判断是否需要显示“切到主路”或“切到辅路”的按钮
     */
    @Getter
    @Setter
    private int flag;

    /**
     * 0：无高架 车标所在道路没有对应的高架路
     * 1：车标在高架上 车标所在道路有对应的高架下道路
     * 2：车标在高架下 车标所在道路有对应的高架上道路
     * 桥上/桥下切换提示:
     * 当hwFlag == 1时，表示车辆当前在高架上，可以显示“切到桥下”按钮（在线模式下）。
     * 当hwFlag == 2时，表示车辆当前在高架下，可以显示“切到桥上”按钮（在线模式下）。
     * 隐藏按钮逻辑:
     * 如果hwFlag == 0，表示当前道路无高架信息，应隐藏“桥上/桥下”切换按钮。
     */
    @Setter
    @Getter
    private int hwFlag;

    private ArrayList<LocalParallelRoadEntity> localParallelRoadEntityArrayList;

    public ArrayList<LocalParallelRoadEntity> getLocalParallelRoadArrayList() {
        return localParallelRoadEntityArrayList;
    }

    public void setLocalParallelRoadArrayList(
            ArrayList<LocalParallelRoadEntity> localParallelRoadEntityArrayList) {
        this.localParallelRoadEntityArrayList = localParallelRoadEntityArrayList;
    }

    @NonNull
    @Override
    public String toString() {
        return "LocParallelInfoEntity{" +
                "status=" + status +
                ", flag=" + flag +
                ", hwFlag=" + hwFlag +
                ", localParallelRoadArrayList=" + localParallelRoadEntityArrayList.toString() +
                '}';
    }
}
