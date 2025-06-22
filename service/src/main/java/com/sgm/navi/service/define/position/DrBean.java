package com.sgm.navi.service.define.position;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.math.BigInteger;

public class DrBean {
    public BigInteger tickTime;
    @LocGNSSStatus.LocGNSSStatus1
    public int gpsStatus;
    public GeoPoint drRawPos;
    public float drAzi;
    public float spd;
    public float posAcc;
    public float aziAcc;
    @LocMoveStatus.LocMoveStatus1
    public int moveStatus;
    @LocDrType.LocDrType1
    public int drStatus;
    public long sceneState;
    public double deltaBearing;
    public double deltaPos;
    public float pluseSpd;
    public float deltaAlt;
    public float deltaAltAcc;
    public float slopeValue;
    public float slopeAcc;
    public double moveDist;
    public boolean bMountAngleReady;
    public boolean matchStatus;
    public GeoPoint drMatchPos;
    public float drMatchAzi;

    public DrBean() {
        this.tickTime = new BigInteger("0");
        this.gpsStatus = 86;
        this.drRawPos = new GeoPoint();
        this.drAzi = 0.0F;
        this.spd = 0.0F;
        this.posAcc = 0.0F;
        this.aziAcc = 0.0F;
        this.moveStatus = 0;
        this.drStatus = 0;
        this.sceneState = 0L;
        this.deltaBearing = 0.0;
        this.deltaPos = 0.0;
        this.pluseSpd = 0.0F;
        this.deltaAlt = 0.0F;
        this.deltaAltAcc = 0.0F;
        this.slopeValue = 0.0F;
        this.slopeAcc = 0.0F;
        this.moveDist = 0.0;
        this.bMountAngleReady = false;
        this.matchStatus = false;
        this.drMatchPos = new GeoPoint();
        this.drMatchAzi = 0.0F;
    }

    public DrBean(BigInteger tickTimeLiteObj, @LocGNSSStatus.LocGNSSStatus1 int gpsStatusLiteObj, GeoPoint drRawPosLiteObj,
                  float drAziLiteObj, float spdLiteObj, float posAccLiteObj, float aziAccLiteObj, @LocMoveStatus.LocMoveStatus1 int moveStatusLiteObj,
                  @LocDrType.LocDrType1 int drStatusLiteObj, long sceneStateLiteObj, double deltaBearingLiteObj, double deltaPosLiteObj,
                  float pluseSpdLiteObj, float deltaAltLiteObj, float deltaAltAccLiteObj, float slopeValueLiteObj, float slopeAccLiteObj,
                  double moveDistLiteObj, boolean bMountAngleReadyLiteObj, boolean matchStatusLiteObj, GeoPoint drMatchPosLiteObj, float drMatchAziLiteObj) {
        this.tickTime = tickTimeLiteObj;
        this.gpsStatus = gpsStatusLiteObj;
        this.drRawPos = drRawPosLiteObj;
        this.drAzi = drAziLiteObj;
        this.spd = spdLiteObj;
        this.posAcc = posAccLiteObj;
        this.aziAcc = aziAccLiteObj;
        this.moveStatus = moveStatusLiteObj;
        this.drStatus = drStatusLiteObj;
        this.sceneState = sceneStateLiteObj;
        this.deltaBearing = deltaBearingLiteObj;
        this.deltaPos = deltaPosLiteObj;
        this.pluseSpd = pluseSpdLiteObj;
        this.deltaAlt = deltaAltLiteObj;
        this.deltaAltAcc = deltaAltAccLiteObj;
        this.slopeValue = slopeValueLiteObj;
        this.slopeAcc = slopeAccLiteObj;
        this.moveDist = moveDistLiteObj;
        this.bMountAngleReady = bMountAngleReadyLiteObj;
        this.matchStatus = matchStatusLiteObj;
        this.drMatchPos = drMatchPosLiteObj;
        this.drMatchAzi = drMatchAziLiteObj;
    }
}
