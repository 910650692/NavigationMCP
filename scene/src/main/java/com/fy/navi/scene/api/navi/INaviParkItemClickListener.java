package com.fy.navi.scene.api.navi;

import com.fy.navi.service.define.navi.NaviParkingEntity;

public interface INaviParkItemClickListener {
    void onItemClick(int listSize, int position, NaviParkingEntity entity);

    void onNaviClick(int position, NaviParkingEntity entity);
}
