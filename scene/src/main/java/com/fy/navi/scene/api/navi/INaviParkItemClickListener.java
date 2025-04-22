package com.fy.navi.scene.api.navi;

import com.fy.navi.service.define.search.PoiInfoEntity;

public interface INaviParkItemClickListener {
    void onItemClick(int position);

    void onNaviClick(int position, PoiInfoEntity entity);
}
