package com.fy.navi.service.logicpaket.hud;

import android.os.Bundle;
import android.view.MotionEvent;

import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IHudCallback {
    void onEGLScreenshot(MapType mapTypeId, byte[] bytes);

}
