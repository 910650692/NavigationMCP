package com.sgm.navi.service.define.aos;

import java.io.Serializable;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyGTraEventDetail extends FyGSubTraEventDetail implements Serializable {
    public FyGTraEventDetail() {
    }

    public int subdetailcount;// 有详情的子事件个数 （hasdetail=1，没有赋值的时候值为1） 非聚合事件为0
    public int subcount;//子事件个数 非聚合事件为0
    public ArrayList<FyGSubTraEventDetail> subinfo = new ArrayList<>();
    public boolean isRequestSuccess = false;
    public long taskId;

    public boolean hasChild() {
        return subcount > 0;
    }

    // 是否可以滚动
    public boolean canScroller() {
        return subcount > 1;
    }
}
