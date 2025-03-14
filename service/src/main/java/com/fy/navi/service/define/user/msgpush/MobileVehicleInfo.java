package com.fy.navi.service.define.user.msgpush;

public class MobileVehicleInfo {
    // 车辆类型 @range [ 0: 客车（默认值）, 1: 货车, 2: 纯电动客车, 3: 纯电动货车, 4: 插电式混动客车, 5: 插电式混动货车 ]
    public int type;
    // 货车大小 @range [ 1: 微型车, 2: 轻型车（默认值）, 3: 中型车, 4: 重型车 ]
    public int size;
    // 货车高度，单位：米
    public double height;
    // 货车宽度，单位：米
    public double width;
    // 货车总重，单位：吨
    public double load;
    // 货车核定载重，单位：吨
    public double weight;
    // 货车轴数，单位：个
    public int axis;
    // 车牌号（UTF8编码）
    public String plate;

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getLoad() {
        return load;
    }

    public void setLoad(double load) {
        this.load = load;
    }

    public double getWeight() {
        return weight;
    }

    public void setWeight(double weight) {
        this.weight = weight;
    }

    public int getAxis() {
        return axis;
    }

    public void setAxis(int axis) {
        this.axis = axis;
    }

    public String getPlate() {
        return plate;
    }

    public void setPlate(String plate) {
        this.plate = plate;
    }
}
