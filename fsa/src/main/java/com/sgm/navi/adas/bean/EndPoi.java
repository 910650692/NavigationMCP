package com.sgm.navi.adas.bean;

class EndPoi {
    private String id;
    private String name;
    private ParkingInfo parkingInfo;
    private int type;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ParkingInfo getParkingInfo() {
        return parkingInfo;
    }

    public void setParkingInfo(ParkingInfo parkingInfo) {
        this.parkingInfo = parkingInfo;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }
}
