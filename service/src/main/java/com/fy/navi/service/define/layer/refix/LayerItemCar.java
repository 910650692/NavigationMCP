package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 *
 */
@Setter
@Getter
public class LayerItemCar extends LayerItemBase {

    private CarModeType carModeType = CarModeType.CAR_MODEL_TYPE_2D;

    private CarSceneType carType = CarSceneType.SCENE_DEFAULT;

    public LayerItemCar(CarModeType carModeType) {
        this.carModeType = carModeType;
    }
}
