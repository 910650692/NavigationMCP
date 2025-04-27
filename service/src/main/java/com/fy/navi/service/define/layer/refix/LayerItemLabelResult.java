package com.fy.navi.service.define.layer.refix;



import androidx.annotation.StringDef;

import com.fy.navi.service.define.bean.GeoPoint;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class LayerItemLabelResult extends LayerItemData {

    /*======终点扎标  1.终点可停车 =====*/

    @ILabelLayerPointType
    private String pointType;   //扎点类型
    private GeoPoint pos;    //终点坐标

    /*============终点扎标===========*/

    @Retention(RetentionPolicy.CLASS)
    @StringDef({ILabelLayerPointType.LABEL_POINT_TYPE_PARK})
    public @interface ILabelLayerPointType {
        /**
         * 终点停车场
         */
        String LABEL_POINT_TYPE_PARK = "PARK";
    }

    @Override
    public String toString() {
        return "LayerItemLabelResult{" +
                "pointType='" + pointType + '\'' +
                ", pos=" + pos +
                '}';
    }
}
