package com.sgm.navi.scene.api.navi;

import com.sgm.navi.service.define.search.PoiInfoEntity;

public interface INaviParkItemClickListener {
    void onItemClick(int position);

    void onNaviClick(int position, PoiInfoEntity entity);
}
