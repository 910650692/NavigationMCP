package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class LayerItemCarSpeedData extends LayerItemData {
    private int speed;

    public LayerItemCarSpeedData(int speed) {
        this.speed = speed;
    }
}
