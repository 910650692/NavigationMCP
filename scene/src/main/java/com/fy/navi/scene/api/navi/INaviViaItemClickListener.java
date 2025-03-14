package com.fy.navi.scene.api.navi;

import com.fy.navi.service.define.navi.NaviViaEntity;

public interface INaviViaItemClickListener {
    void onItemClick(int position, NaviViaEntity entity);

    void onDelClick(int position, NaviViaEntity entity);
}
