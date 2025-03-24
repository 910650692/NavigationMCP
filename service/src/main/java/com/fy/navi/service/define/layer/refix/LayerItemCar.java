package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 *
 */
@Setter
@Getter
public class LayerItemCar extends LayerItemBase {

    private int speed;

    public int getSpeed() {
        return speed;
    }

    public void setSpeed(int speed) {
        this.speed = speed;
    }
}
