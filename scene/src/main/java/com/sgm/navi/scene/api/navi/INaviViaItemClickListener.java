package com.sgm.navi.scene.api.navi;

import com.sgm.navi.service.define.navi.NaviViaEntity;

public interface INaviViaItemClickListener {
    void onItemClick(int position, NaviViaEntity entity);

    void onDelClick(int position, NaviViaEntity entity);
}
