package com.sgm.navi.service.logicpaket.hud;

import com.sgm.navi.service.define.map.MapType;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IHudCallback {
    void onEGLScreenshot(MapType mapTypeId, byte[] bytes);

}
