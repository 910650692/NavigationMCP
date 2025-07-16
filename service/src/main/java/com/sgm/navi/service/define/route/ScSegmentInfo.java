package com.sgm.navi.service.define.route;

import java.util.ArrayList;
import java.util.List;

public class ScSegmentInfo {
    private long pathId;
    private List<ScSegment> scSegments = new ArrayList<>();

    public long getPathId() {
        return pathId;
    }

    public void setPathId(long pathId) {
        this.pathId = pathId;
    }

    public List<ScSegment> getScSegments() {
        return scSegments;
    }

    public void addScSegment(ScSegment scSegment) {
        this.scSegments.add(scSegment);
    }

    public static class ScSegment {
        private long index; // Segment的index
        private int roadClass;
        private List<ScSpeedLimit> scSpeedLimits = new ArrayList<>();
        private long length; // Segment的length

        public long getIndex() {
            return index;
        }

        public void setIndex(long index) {
            this.index = index;
        }

        public int getRoadClass() {
            return roadClass;
        }

        public void setRoadClass(int roadClass) {
            this.roadClass = roadClass;
        }

        public List<ScSpeedLimit> getScSpeedLimits() {
            return scSpeedLimits;
        }

        public void addScSpeedLimits(ScSpeedLimit scSpeedLimit) {
            this.scSpeedLimits.add(scSpeedLimit);
        }

        public long getLength() {
            return length;
        }

        public void setLength(long length) {
            this.length = length;
        }
    }

    public static class ScSpeedLimit {
        private long speed;
        private long offset;

        public long getSpeed() {
            return speed;
        }

        public void setSpeed(long speed) {
            this.speed = speed;
        }

        public long getOffset() {
            return offset;
        }

        public void setOffset(long offset) {
            this.offset = offset;
        }
    }
}
