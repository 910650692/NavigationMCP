package com.sgm.navi.service.define.map;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MapVisibleAreaPoint {

    private MapVisibleAreaInfo map_main_car;
    private MapVisibleAreaInfo map_main_naving;
    private MapVisibleAreaInfo map_main_setting;
    private MapVisibleAreaInfo map_main_car_window;
    private MapVisibleAreaInfo map_main_naving_window;
    private MapVisibleAreaInfo map_main_setting_window;

}
