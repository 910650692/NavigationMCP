package com.fy.navi.scene.api.navi;

import com.fy.navi.service.define.navi.NaviParkingEntity;

public interface INaviParkItemClickListener {
    void onItemClick(int position);

    void onNaviClick(int position, NaviParkingEntity entity);
}
